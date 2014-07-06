/**
 * Autogenerated by Thrift Compiler (0.9.1)
 *
 * DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
 *  @generated
 */
using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.IO;
using Thrift;
using Thrift.Collections;
using System.ServiceModel;
using System.Runtime.Serialization;
using Thrift.Protocol;
using Thrift.Transport;

namespace FlexSearch.Api
{

  #if !SILVERLIGHT
  [Serializable]
  #endif
  [DataContract(Namespace="")]
  public partial class FilterList : TBase
  {

    [DataMember(Order = 1)]
    public List<string> Words { get; set; }

    public FilterList() {
      this.Words = new List<string>();
    }

    public FilterList(List<string> Words) : this() {
      this.Words = Words;
    }

    public void Read (TProtocol iprot)
    {
      bool isset_Words = false;
      TField field;
      iprot.ReadStructBegin();
      while (true)
      {
        field = iprot.ReadFieldBegin();
        if (field.Type == TType.Stop) { 
          break;
        }
        switch (field.ID)
        {
          case 1:
            if (field.Type == TType.List) {
              {
                Words = new List<string>();
                TList _list60 = iprot.ReadListBegin();
                for( int _i61 = 0; _i61 < _list60.Count; ++_i61)
                {
                  string _elem62 = null;
                  _elem62 = iprot.ReadString();
                  Words.Add(_elem62);
                }
                iprot.ReadListEnd();
              }
              isset_Words = true;
            } else { 
              TProtocolUtil.Skip(iprot, field.Type);
            }
            break;
          default: 
            TProtocolUtil.Skip(iprot, field.Type);
            break;
        }
        iprot.ReadFieldEnd();
      }
      iprot.ReadStructEnd();
      if (!isset_Words)
        throw new TProtocolException(TProtocolException.INVALID_DATA);
    }

    public void Write(TProtocol oprot) {
      TStruct struc = new TStruct("FilterList");
      oprot.WriteStructBegin(struc);
      TField field = new TField();
      field.Name = "Words";
      field.Type = TType.List;
      field.ID = 1;
      oprot.WriteFieldBegin(field);
      {
        oprot.WriteListBegin(new TList(TType.String, Words.Count));
        foreach (string _iter63 in Words)
        {
          oprot.WriteString(_iter63);
        }
        oprot.WriteListEnd();
      }
      oprot.WriteFieldEnd();
      oprot.WriteFieldStop();
      oprot.WriteStructEnd();
    }

    public override bool Equals(object that) {
      var other = that as FilterList;
      if (other == null) return false;
      if (ReferenceEquals(this, other)) return true;
      return TCollections.Equals(Words, other.Words);
    }

    public override int GetHashCode() {
      int hashcode = 0;
      unchecked {
        hashcode = (hashcode * 397) ^ ((TCollections.GetHashCode(Words)));
      }
      return hashcode;
    }

    public override string ToString() {
      StringBuilder sb = new StringBuilder("FilterList(");
      sb.Append("Words: ");
      sb.Append(Words);
      sb.Append(")");
      return sb.ToString();
    }

  }

}
