using System;
using System.Text;
using System.IO;
using System.Net.Sockets;
using System.Net;
using System.Web.Mail;
namespace SMTP
{
	/// <summary>
	/// provides methods to send email via smtp direct to mail server
	/// </summary>
	public class SmtpDirect
	{
		/// <summary>
		/// Get / Set the name of the SMTP mail server
		/// </summary>
		public static string SmtpServer;
		private enum SMTPResponse: int
		{
			CONNECT_SUCCESS = 220,
			GENERIC_SUCCESS = 250,
			DATA_SUCCESS = 354,
			QUIT_SUCCESS = 221 
		}
		public static bool Send(MailMessage message)
		{
			IPHostEntry IPhst = Dns.Resolve(SmtpServer);
			IPEndPoint endPt = new IPEndPoint(IPhst.AddressList[0], 25);
			Socket s= new Socket(endPt.AddressFamily, SocketType.Stream,ProtocolType.Tcp);
			s.Connect(endPt);

			if(!Check_Response(s, SMTPResponse.CONNECT_SUCCESS))
			{ 
				s.Close();
				return false;
			}

			Senddata(s, string.Format("HELO {0}\r\n", Dns.GetHostName() ));
			if(!Check_Response(s, SMTPResponse.GENERIC_SUCCESS))
			{
				s.Close();
				return false;
			}

			Senddata(s, string.Format("MAIL From: {0}\r\n", message.From ));
			if(!Check_Response(s, SMTPResponse.GENERIC_SUCCESS))
			{

				s.Close();
				return false;
			}

			string _To = message.To;
			string[] Tos= _To.Split(new char[] {';'});
			foreach (string To in Tos)
			{
				Senddata(s, string.Format("RCPT TO: {0}\r\n", To));
				if(!Check_Response(s, SMTPResponse.GENERIC_SUCCESS))
				{ 
					s.Close();
					return false;
				}
			}

			if(message.Cc!=null)
			{
				Tos= message.Cc.Split(new char[] {';'});
				foreach (string To in Tos)
				{
					Senddata(s, string.Format("RCPT TO: {0}\r\n", To));
					if(!Check_Response(s, SMTPResponse.GENERIC_SUCCESS))
					{ 
						s.Close();
						return false;
					}
				}
			}

			StringBuilder Header=new StringBuilder();
			Header.Append("From: " + message.From + "\r\n");
			Tos= message.To.Split(new char[] {';'});
			Header.Append("To: ");
			for( int i=0; i< Tos.Length; i++)
			{
				Header.Append( i > 0 ? "," : "" );
				Header.Append(Tos[i]);
			}
			Header.Append("\r\n");
			if(message.Cc!=null)
			{
				Tos= message.Cc.Split(new char[] {';'});
				Header.Append("Cc: ");
				for( int i=0; i< Tos.Length; i++)
				{
					Header.Append( i > 0 ? "," : "" );
					Header.Append(Tos[i]);
				}
				Header.Append("\r\n");
			}
			Header.Append( "Date: " );

			string[] weekdays = new string[7] {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};
			string[] months = new string[12] {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
			string date_now = String.Format("{0}, {1} {2} {3} {4} +0300 (MSK)",
				weekdays[(int)DateTime.Now.DayOfWeek],
				DateTime.Now.Day, months[DateTime.Now.Month-1], DateTime.Now.Year,
				DateTime.Now.ToString("HH:mm:ss"));

			Header.Append(date_now);
			Header.Append("\r\n");
			Header.Append("Subject: " + message.Subject+ "\r\n");
			Header.Append( "X-Mailer: SMTPDirect v1\r\n" );
			string MsgBody = message.Body;
			if(!MsgBody.EndsWith("\r\n"))
				MsgBody+="\r\n";
			if(message.Attachments.Count>0)
			{
				Header.Append( "MIME-Version: 1.0\r\n" );
				Header.Append( "Content-Type: multipart/mixed; boundary=unique-boundary-1\r\n" );
				Header.Append("\r\n");
				Header.Append( "This is a multi-part message in MIME format.\r\n" );
				StringBuilder sb = new StringBuilder();
				sb.Append("--unique-boundary-1\r\n");
				sb.Append("Content-Type: text/plain\r\n");
				sb.Append("Content-Transfer-Encoding: 7Bit\r\n");
				sb.Append("\r\n");
				sb.Append(MsgBody + "\r\n");
				sb.Append("\r\n");

				foreach(object o in message.Attachments)
				{
					MailAttachment a = o as MailAttachment;
					byte[] binaryData;
					if(a!=null)
					{
						FileInfo f = new FileInfo(a.Filename);
						sb.Append("--unique-boundary-1\r\n");
						sb.Append("Content-Type: application/octet-stream; file=" + f.Name + "\r\n");
						sb.Append("Content-Transfer-Encoding: base64\r\n");
						sb.Append("Content-Disposition: attachment; filename=" + f.Name + "\r\n");
						sb.Append("\r\n");
						FileStream fs = f.OpenRead();
						binaryData = new Byte[fs.Length];
						long bytesRead = fs.Read(binaryData, 0, (int)fs.Length);
						fs.Close();
						string base64String = System.Convert.ToBase64String(binaryData, 0,binaryData.Length);

						for(int i=0; i< base64String.Length ; )
						{
							int nextchunk=100;
							if(base64String.Length - (i + nextchunk ) <0)
								nextchunk = base64String.Length -i;
							sb.Append(base64String.Substring(i, nextchunk));
							sb.Append("\r\n");
							i+=nextchunk; 
						}
						sb.Append("\r\n"); 
					}
				}
				MsgBody=sb.ToString();
			}

			Senddata(s, ("DATA\r\n"));
			if(!Check_Response(s, SMTPResponse.DATA_SUCCESS))
			{ 
				s.Close();
				return false;
			}
			Header.Append( "\r\n" );
			Header.Append( MsgBody );
			Header.Append( ".\r\n" );
			Header.Append( "\r\n" );
			Header.Append( "\r\n" );
			Senddata(s, Header.ToString());
			if(!Check_Response(s, SMTPResponse.GENERIC_SUCCESS ))
			{ 
				s.Close();
				return false;
			} 

			Senddata(s, "QUIT\r\n");
			Check_Response(s, SMTPResponse.QUIT_SUCCESS );
			s.Close(); 
			return true;
		}
		private static void Senddata(Socket s, string msg)
		{ 
			byte[] _msg = Encoding.ASCII.GetBytes(msg);
			s.Send(_msg , 0, _msg .Length, SocketFlags.None);
		}
		private static bool Check_Response(Socket s, SMTPResponse response_expected )
		{
			string sResponse;
			int response;
			byte[] bytes = new byte[1024];
			while (s.Available==0)
			{
				System.Threading.Thread.Sleep(100);
			}

			s.Receive(bytes, 0, s.Available, SocketFlags.None);
			sResponse = Encoding.ASCII.GetString(bytes);
			response = Convert.ToInt32(sResponse.Substring(0,3));
			if(response != (int)response_expected)
				return false;
			return true;
		}
	}
} 